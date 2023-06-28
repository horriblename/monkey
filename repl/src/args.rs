use std::path::PathBuf;

#[derive(Default)]
pub struct Config {
    pub in_file: Option<PathBuf>,
}

type ArgsResult = Result<Config, ArgsError>;

#[derive(Debug)] // TODO: impl Debug
pub enum ArgsError {
    MissingArg { flag: String },
    MultipleInFile,
    UnknownArg { arg: String },
}

pub fn read_args() -> ArgsResult {
    let mut config: Config = Default::default();

    let mut args = std::env::args().skip(1).into_iter();
    while let Some(arg) = args.next() {
        let arg = &arg;
        eprintln!("{arg}");
        match arg.as_str() {
            "-f" | "--fromfile" => {
                if config.in_file.is_some() {
                    return Err(ArgsError::MultipleInFile);
                }
                config.in_file = Some(PathBuf::from(
                    args.next()
                        .ok_or(ArgsError::MissingArg { flag: arg.clone() })?,
                ));
            }
            arg => {
                return Err(ArgsError::UnknownArg {
                    arg: arg.to_string(),
                })
            }
        }
    }

    Ok(config)
}
