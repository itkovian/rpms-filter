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
///     - locked: path to file with list of locked packages
///     - remove: path to file with removal candidates' absolute path
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
    let regex_pattern = pattern.replace("*", "[-a-zA-Z0-9_.]*");
    let regex = Regex::new(&regex_pattern.as_str()).unwrap();
    (prefix, regex)
}


fn remove<'a>(locked_packages: Vec<&str>, removal_candidates: Vec<(&'a str, &'a str)>) -> Vec<&'a str> {

    let mut removals : Vec<&str> = Vec::new();
    let mut candidates : Vec<(&str, &str)> = removal_candidates.clone();

    for lp in locked_packages.iter() {
        //println!("Checking locked package {}", &lp);

        let (prefix, regex) : (&str, Regex) = regexify(lp);
        //println!("----> prefix: {}, regex: {:?}", &prefix, &regex);
        //println!("----> candidates: {:?}", candidates);

        // these can never be matched, since they come before the prefix
        let mut rs : Vec<&str> = candidates.iter()
                                           .take_while(|&&(bn , _)| bn < &prefix)
                                           .map(|&t| t.1)
                                           .collect();
        //println!("----> Removing {:?}", &rs);
        removals.append(&mut rs);

        // we drop the ones matching the current regex, since these are locked
        candidates = candidates.into_iter()
                               .skip_while(|&(bn, _)| bn < &prefix)
                               .skip_while(|&(bn, _)| regex.is_match(bn))
                               .collect();
        //println!("----> {} candidates left", candidates.len());
    }
    let mut remainder : Vec<&str> = candidates.into_iter().map(|t| t.1).collect();
    removals.append(&mut remainder);
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
    let removal_candidates_ : Vec<&str> = removal_lines.iter().map(|s| s.as_str()).collect();
    let mut removal_candidates : Vec<(&str, &str)> = removal_candidates_.clone()
        .iter()
        .map(|&s| Path::new(s).file_name().unwrap().to_str().unwrap())
        .zip(
            removal_candidates_.clone()
            .iter()
            .map(|s| *s)
        ).collect();

    removal_candidates.sort();

    let removals = remove(locked_packages, removal_candidates);

    for p in removals.iter() {
        println!("{}", p);
    }
}
