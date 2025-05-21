import shellout
import tailwind

pub fn main() {
  let assert Ok(_) = tailwind.install()
  let assert Ok(_) =
    tailwind.run([
      "--input", "./src/fable.css", "--output", "./build/fable.min.css",
      "--minify",
    ])

  let assert Ok(_) =
    shellout.command(run: "gleam", with: ["run", "-m", "cog"], in: ".", opt: [])
}
