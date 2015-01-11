extern crate noise;

use noise::{simplectic2, simplectic3, simplectic4, Seed, Point2};

fn render(x:u8) -> char {
  match x / 53 {
    0 => ' ',
    1 => ' ',
    2 => '▩',
    3 => '█',
    _ => '█' }}

fn main() {
  for i in range(0, 25) {
    for j in range(0, 50) {
      let x = (i*4) as f32;
      let y = (j*4) as f32;
      let v = simplectic2(&Seed::new(9), &[x, y]);
      let r = (128.0 + v*256.0) as u8;
      print!("{}", render(r)); }
    println!(""); }}
