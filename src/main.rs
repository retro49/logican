extern crate logican as lg;

fn start_logican(buff: &Vec<u8>) {
    let mut lexer = lg::lexer::Lexer::new(buff.as_ref());
    let mut prsr = lg::parser::Parser::new(&mut lexer);
    let mut _res = prsr.parse_solution().unwrap();
    // lg::eval::Evaluator::default().eval(&mut res);
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
