import React from 'react';

class ConsoleCurrentLine extends React.Component {
    render() {
        return (
            <p>{this.props.prompt}{this.props.children}<span className="cursor"></span></p>
        );
    }
};

export default ConsoleCurrentLine;
