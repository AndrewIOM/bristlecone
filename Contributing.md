# Contributing

Thank you for your interest in contributing. Bristlecone welcomes improvements of all kinds — bug fixes, documentation updates, new examples, and feature suggestions.

## How to contribute

- **Open an issue** if you’ve found a bug, want to request a feature, or need clarification.
- **Fork the repository** and create a new branch for your changes.
- **Make your changes** in a clear and focused way. Smaller pull requests are easier to review.
- **Open a pull request** describing what you changed and why.

## Code style

Please follow the existing style and structure of the project. If in doubt, keep things simple and consistent with the surrounding code. Bristlecone uses fantomas formatting for the src folder, so you must run `dotnet fantomas ./src` on any incoming code.

## Tests

If your change affects behaviour, please add or update tests where appropriate. We prefer property tests over simple case tests where possible.

## Adding features

If you're adding a new public-facing feature, please  consider adding examples that can be turned into documentation. The project uses fsdocs literal scripting, so any examples placed in docs/examples/ will be executed when docs are compiled on a successful merge.

## Communication

All contributions are reviewed in good faith. Please be respectful and constructive.