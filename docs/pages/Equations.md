## Creating Equations with Daum Equation Editor

Occasionally you will need to type equations in homework and labs. R Markdown supports LaTeX style equations using the [MathJax](https://www.mathjax.org/) javascript library. I do not expect you to learn LaTeX for this course. Instead, I recommend using the free application [Daum Equation Editor](). It availabe [online](https://www.mathjax.org/), as a [Google Chrome Extension](https://chrome.google.com/webstore/detail/daum-equation-editor/dinfmiceliiomokeofbocegmacmagjhe?hl=en), or as a standalone [Mac Application](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=3&cad=rja&uact=8&ved=0CC4QFjACahUKEwiegvqQod3GAhUCmYAKHctnA5M&url=https%3A%2F%2Fitunes.apple.com%2Fus%2Fapp%2Fdaum-equation-editor%2Fid540665783%3Fmt%3D12&ei=WGKmVZ72DIKyggTLz42YCQ&usg=AFQjCNH69beGYAfr5ojnPILLNOG-goN9sw&sig2=Qw7QV80X8gZjrNyY9PkMHA). With the editor, you can enter quations using menus. 

![Daum Equation Editor Screenshot](DaumScreen.png)

Once done, copy and paste the LaTeX code at teh bottom into your R Markdown file between two dollar signs (i.e. $), and the quation will be rendered by the web browser.

$$ f\left( x|\mu ,\sigma  \right) =\frac { 1 }{ \sigma \sqrt { 2\pi  }  } { e }^{ -\frac { { \left( x-\mu  \right)  }^{ 2 } }{ { 2\sigma  }^{ 2 } }  } $$

