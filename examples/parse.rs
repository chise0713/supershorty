//! Output:
//! ```text
//! usage: myapp [-bh] [-a string] [-c f32]
//! Command Summary:
//!         -a              accept string
//!         -b              and boolean
//!         -c              and someother, e.g. f32
//!         -h              prints this help message
//! ```

use supershorty::Args;

#[derive(Args, Debug)]
#[name("myapp")]
struct MyArgs {
    #[arg(flag = 'a', help = "accept string")]
    string: Option<Box<str>>,
    #[arg(flag = 'b', help = "and boolean")]
    boolean: bool,
    #[arg(flag = 'c', help = "and someother, e.g. f32")]
    f32: Option<f32>,
}

fn main() {
    let args = MyArgs::parse();
    println!("Parsed arguments: {:?}", args);
}
