import React from 'react';

class ConsoleLogLine extends React.Component {
    render() {
        return (
            <p>{this.props.children}</p>
        );
    }
};

export default ConsoleLogLine;
