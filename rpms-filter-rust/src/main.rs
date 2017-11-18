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

    let mut locked_lines : Vec<String> = get_lines(Path::new(opts.value_of("locked").unwrap())).unwrap();
    let locked_packages : Vec<String> = locked_lines.iter_mut()
                                                  .map(|s| {
                                                      let s_ = s.split(',').nth(0).unwrap();
                                                      let s__ = s_.replace("*", "[-a-zA-Z0-9_.]*");
                                                      s__
                                                  })
                                                  .collect();
    let locked_regex_pattern = ["(", locked_packages.join("|").as_str(), ")"].concat();
    let regex = Regex::new(&locked_regex_pattern.as_str()).unwrap();

    let removal_lines : Vec<String> = get_lines(Path::new(opts.value_of("remove").unwrap())).unwrap();
    let removal_candidates : Vec<&str> = removal_lines.iter().map(|s| s.as_str()).collect();
    let removals : Vec<&str> = removal_candidates.into_iter()
                                     .filter(|&p| ! regex.is_match(p))
                                     .collect();

    for p in removals.iter() {
        println!("{}", p);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_remove() {

        let locked = vec![
            "kernel-firmware-2.6.32-431.29.2.el6",
            "Lmod-*.ug*",
            "gpfs.gskit-8.0.50-*",
            "kernel*-3.10.0-327.28.3.el7"
        ];

        let rcs = vec![
            ("torque-server-3.0.2-el6.ug.2.x86_64.rpm", "/testrepo/torque-server-3.0.2-el6.ug.2.x86_64.rpm"),
            ("kernel-firmware-3.10.0-327.28.3.el7.x86_64.rpm", "/testrepo/kernel-firmware-3.10.0-327.28.3.el7.x86_64.rpm"),
            ("torque-server-3.0.8-el6.ug.2.x86_64.rpm", "/testrepo/torque-server-3.0.8-el6.ug.2.x86_64.rpm")
        ];

        let removals = remove(locked, rcs);

        assert!(removals[0] == "/testrepo/torque-server-3.0.2-el6.ug.2.x86_64.rpm");
        //assert!(removals[1] == "/testrepo/torque-server-3.0.8-el6.ug.2.x86_64.rpm");

    }
}
