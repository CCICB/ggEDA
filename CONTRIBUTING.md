# NA

### Contributing to ggEDA

There are many ways to contribute to ggEDA.

1.  Request features you would like to by [creating new issues on
    github](https://github.com/CCICB/ggEDA/issues)
2.  [Make your visualisation packages
    ggEDA-compatible](#make-your-visualisation-packages-ggeda-compatible)
3.  [Directly contribute to the ggEDA
    codebase](#directly-contribute-to-the-ggeda-codebase)

#### Make your visualisation packages ggEDA-compatible

If your package produces ggplots that you would like to interactively
link with ggEDA visualisations, consider converting your geoms to their
ggiraph interactive equivalents and adding a data_id based the same
values you supply to ggEDA via the `col_id` argument. That way end-users
can create a data-linked ggEDA plot composed with your packages plots
using patchwork.

A detailed description of cross-linking between ggEDA plots and other
visualisations is available \[here\].
(<https://CCICB.github.io/ggEDA/articles/advanced_interactivity.html#cross-linking-ggEDA-plots-with-other-visualisations>)

#### Directly contribute to the ggEDA codebase

We welcome contributions from the community to enhance and expand the
functionality of `ggEDA`. Whether you want to fix a bug, add new
features, improve documentation, or optimize performance, your efforts
are highly valued. To get started:

1.  **Fork the Repository**: Click on the ‘Fork’ button at the top right
    of this page to create a copy of the repository in your GitHub
    account.

2.  **Clone the Repository**: Use `git clone` to clone your forked
    repository to your local machine.

    ``` bash
    git clone https://github.com/CCICB/ggEDA.git
    ```

3.  **Create a Branch**

    ``` bash
    git checkout -b feature-name
    ```

4.  **Make Changes**: Implement your changes in the new branch

5.  **Commit and Push**: Commit your changes and push the branch to your
    forked repository.

6.  **Create a Pull Request**: Go to the original repository and open a
    pull request from your branch. Please provide a clear description of
    your changes and any relevant issues or discussions.

### Report Issues or Problems with the Software

If you encounter any issues, bugs, or have suggestions for improvements,
please report them using the [GitHub Issues
Tab](https://github.com/CCICB/ggEDA/issues/).

### Seek Support

For any questions or support regarding the use of ggEDA you can:

- **Check the Documentation**: Comprehensive documentation is available
  [here](https://CCICB.github.io/ggEDA/index.html).

- **Create a** [new issue](https://github.com/CCICB/ggEDA/issues/new)
  with your query.

- **Browse Existing Issues**: Check the
  [Issues](https://github.com/CCICB/ggEDA/issues) page to see if your
  query has been addressed.

- **Contact Us**: If you need direct assistance, please [contact the
  maintainers directly](mailto:CCICB@ccia.org.au?subject=ggEDA)
