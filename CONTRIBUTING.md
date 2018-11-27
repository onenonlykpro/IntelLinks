# Contributing to Atheneos

First, thanks for taking the time to contribute!

The following is a set of guidelines for contributing to Atheneos.  Feel free to propose changes to this document in a pull request.

## Any apps must be able to store user inputs offline
Because Athenous may be used by intelligence analysts and researchers who are privy to confidential or private information, any app in Atheneos must be functional in an offline setting.  There should be no uploading of user inputs to a database.  

## Users must be warned of apps that cannot run offline
If an app cannot be run offline (i.e. a Twitter account analysis tool), then it must be made clear in the README.md file of that app.

## New apps should be should in the Projects tab
If you have a new idea for an Atheneos app, please propose it in the Project tab of this repository.

## App enhancements should be submitted as an issue
Enhancement suggestions are tracked as GitHub issues. After you've determined which app your enhancement suggestion is related to, create an issue on that app's repository and provide the following information:
* Use a clear and descriptive title for the issue to identify the suggestion.
* Provide a step-by-step description of the suggested enhancement in as many details as possible.
* Provide specific examples to demonstrate the steps. Include copy/pasteable snippets which you use in those examples, as Markdown code blocks.
* Describe the current behavior and explain which behavior you expected to see instead and why.
* Include screenshots and animated GIFs which help you demonstrate the steps or point out the part of Atheneos which the suggestion is related to. 
* Explain why this enhancement would be useful to most Atheneos users and isn't something that can or should be implemented as a community package.
* List some other applications where this enhancement exists.

## Codify your Git commits
Please utilize the following guidelines when making a Git commit
* Use the present tense ("Add feature" not "Added feature")
* Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
* Limit the first line to 72 characters or less
* Reference issues and pull requests liberally after the first line
* Start the commit message with an applicable emoji:
  * :art: - `:art:` when improving the format/structure of the code
  * :racehorse: - `:racehorse:` when improving performance
  * :memo: - `:memo:` when writing docs
  * :bug: - `:bug:` when fixing a bug
  * :fire: - `:fire:` when removing code or files
  * :lock: - `:lock:` when dealing with security
