import React from 'react';

class ConsoleCurrentLine extends React.Component {
    componentDidUpdate() {
        React.findDOMNode(this).scrollIntoView();
    }
    
    render() {
        return (
            <pre>{this.props.prompt}{this.props.string}<span className="cursor"></span></pre>
        );
    }
};

export default ConsoleCurrentLine;
