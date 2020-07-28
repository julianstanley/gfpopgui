/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

class Footer extends React.Component {
  docUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    const docsUrl = this.props.config.docsUrl;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
    const langPart = `${language ? `${language}/` : ''}`;
    return `${baseUrl}${docsPart}${langPart}${doc}`;
  }

  pageUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    return baseUrl + (language ? `${language}/` : '') + doc;
  }

  render() {
    return (
      <footer className="nav-footer" id="footer">
        <section className="sitemap">
          <a href={this.props.config.baseUrl} className="nav-home">
            {this.props.config.footerIcon && (
              <img
                src={this.props.config.baseUrl + this.props.config.footerIcon}
                alt={this.props.config.title}
                width="66"
                height="58"
              />
            )}
          </a>
          <div>
            <h5>Content</h5>
            <a href={this.docUrl('tutorials/intro_tutorials.html')}>
              Tutorials
            </a>
            <a href={this.docUrl('timeline/intro_timeline.html')}>
              Project Timeline
            </a>
          </div>
          <div>
            <h5>More Information</h5>
            <a href='https://summerofcode.withgoogle.com/projects/#6502959753461760'>
              Google Summer of Code Page
            </a>
            <a href='https://julianstanley.com/'>
              About Julian
            </a>
          </div>
          <div>
            <h5>GitHub</h5>
            <a href="https://github.com/julianstanley/gfpopgui">gfpopgui</a>
            <a href="https://github.com/vrunge/gfpop">gfpop</a>
          </div>
        </section>
      </footer>
    );
  }
}

module.exports = Footer;
