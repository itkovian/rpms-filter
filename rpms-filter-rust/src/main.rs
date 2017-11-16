#[macro_use]
extern crate clap;
extern crate regex;
extern crate version;

use clap::{Arg, App, ArgMatches};
use regex::Regex;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

/// The options for the rpms filter
///
///     - path: can take multiple value,
///     metadata: s
///     - log: boolean indicating if events should be logged
fn get_opts<'a>() -> ArgMatches<'a> {
    App::new("RPM filter")
        .version(crate_version!())
        .author("Andy Georges <itkovian@gmail.com>")
        .about("Print RPMs to keep in the repository based on a locked list")
        .arg(Arg::with_name("locked")
             .short("l")
             .long("locked")
             .number_of_values(1)
             .required(true)
             .help("The path to the file with the locked RPMs"))
        .arg(Arg::with_name("remove")
             .short("r")
             .long("remove")
             .number_of_values(1)
             .required(true)
             .help("The path to the file with the removal candidates"))
        .get_matches()
}


fn regexify<'a>(pattern: &'a str) -> (&'a str, Regex) {

    let prefix = pattern.split("*").nth(0).unwrap();
    pattern.replace("*", "[-a-zA-Z0-9_.]*");
    let regex = Regex::new(pattern).unwrap();

    (prefix, regex)
}


fn remove<'a>(locked_packages: Vec<&str>, removal_candidates: Vec<&'a str>) -> Vec<&'a str> {

    let mut removals : Vec<&str> = Vec::new();
    let mut candidates : Vec<&str> = removal_candidates.clone();

    for lp in locked_packages.iter() {

        let (prefix, regex) : (&str, Regex) = regexify(lp);

        // these can never be matched, since they come before the prefix
        let mut rs : Vec<&str> = candidates.iter()
                                       .cloned()
                                       .take_while(|s| s < &prefix).collect();
        removals.append(&mut rs);

        // we drop the ones matching the current regex, since these are locked
        candidates = candidates.iter()
                               .cloned()
                               .skip_while(|s| s < &prefix)
                               .skip_while(|s| regex.is_match(s))  // CHECK: we have sorted values, so the first non match should break ?
                               .collect();
    }

    removals
}

// Return the lines in a file as an iterator
fn get_lines<P>(path: P) -> Result< Vec<String>, io::Error>
where
    P: AsRef<Path>,
{
    let mut file = File::open(path).unwrap();
    let mut buffer = String::new();

    file.read_to_string(&mut buffer).unwrap();

    Ok(buffer.lines().map(ToOwned::to_owned).collect())
}

fn main() {
    let opts = get_opts();

    let locked_lines : Vec<String> = get_lines(Path::new(opts.value_of("locked").unwrap())).unwrap();
    let mut locked_packages: Vec<&str> = locked_lines.iter()
                                                     .map(|s| s.split(',').nth(0).unwrap())
                                                     .collect();

    locked_packages.sort();

    let removal_lines : Vec<String> = get_lines(Path::new(opts.value_of("remove").unwrap())).unwrap();
    let mut removal_candidates : Vec<&str> = removal_lines.iter().map(|s| s.as_str()).collect();
    removal_candidates.sort();

    let removals = remove(locked_packages, removal_candidates);

    removals.iter().map(|s| print!("{}", s));
}
