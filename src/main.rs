extern crate logican as lg;

fn start_logican(buff: &Vec<u8>) {
    let mut lexer = lg::lexer::Lexer::new(buff.as_ref());
    let mut prsr = lg::parser::Parser::new(&mut lexer);
    let interpreter = lg::interpreter::Interpreter::interpret(&mut prsr);
    match interpreter {
        Ok(_) => {}
        Err(e) => {
            println!("error has occured during interpretation");
            println!("{}", e);
        }
    };
}

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.len() < 1 {
        println!("error: no file provided");
    }
    for file in &args {
        let path = std::path::Path::new(file);
        let stream = lg::fr::read_file(path);
        start_logican(&stream);
    }
}
