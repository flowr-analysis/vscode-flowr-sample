# *flowR* Sample Codespace

This is a template repository that you can use to try out the *flowR* extension for Visual Studio Code right in your browser using [GitHub codespaces](https://github.com/features/codespaces). You don't even need to create your own repository!

<div align = center>
<br>

[![](https://img.shields.io/badge/Create_a_flowR_Codespace-a32638?style=flat&logo=github)](https://github.com/codespaces/new?template_repository=flowr-analysis/vscode-flowr-sample)

<br>
</div>

## How to Use

To set up your own *flowR* sample codespace, simply click the button above. The codespace will take a few seconds to set up. After the codespace has fully loaded, the *flowR* extension will connect to a remote *flowR* server, so you'll be able to do anything that you can do with the *flowR* extension on Desktop.

## Things to Try

You can generate a [slice](https://github.com/flowr-analysis/flowr/wiki/Terminology#program-slice) of the currently highlighted variable in any of the sample code by using the **Slice for Cursor Position** command. All code that is not part of the generated slice will then be grayed out.

You can also view the reconstruction of a piece of code based on the current slice. The **Show Current Slice in Editor (Reconstruct)** command opens a view next to the current editor.

The extension has a lot more features to try! For a more extensive list, check out [the extension's README](https://github.com/flowr-analysis/vscode-flowr?tab=readme-ov-file#use).

### Example Use-Cases

This codespace contains some randomly selected R files under [`./sample-files`](./sample-files/) you can use to play around - or use your own R files!
We have only modified these files to contain a meta-informational comment at the top, pointing to the full record.

1. Reviewing and comprehending existing R scripts
2. Reusing a figure or data cleaning step of a publication
3. Supporting maintenance and reproducibility
4. Interactively understand the impact of parts while developing the script
