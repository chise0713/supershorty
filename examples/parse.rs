//! Output:
//! ```text
//! usage: myapp [-bh] [-a string] [-c f32]
//! Command Summary:
//!         -a              accept string
//!         -b              and boolean
//!         -c              and someother, e.g. f32
//!         -h              prints this help message
//! ```

use std::process::ExitCode;

use supershorty::Args;

#[derive(Args, Debug)]
#[args(name = "myapp", allow_no_args = true)]
struct MyArgs {
    #[arg(flag = 'a', help = "accept string")]
    string: Option<Box<str>>,
    #[arg(flag = 'b', help = "and boolean")]
    boolean: bool,
    #[arg(flag = 'c', help = "and someother, e.g. f32")]
    f32: Option<f32>,
}

fn main() -> ExitCode {
    let args = match MyArgs::parse() {
        Ok(args) => args,
        Err(e) => {
            return e;
        }
    };
    println!("Parsed arguments: {:?}", args);
    ExitCode::SUCCESS
}
