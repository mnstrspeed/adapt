# Adapt
Writing 101: know your audience. Adapt lets you tailor documents for different audiences using a simple text format. For example, this sentence only shows up on GitHub.

## Format
The format of `.adapt` files is primarly described by the source in `Main.hs`.

	The glass is half [optimist => full|pessimist => empty]

## Usage
To use adapt:

	adapt <target> c1 c2 ... cn

where `<target>` should have a corresponding file `<target>.adapt` and `c1 c2 ... cn` should be a list of tags. The result will be stored in `<target>`.

### Example
To adapt this README for GitHub:

	adapt README.md github
