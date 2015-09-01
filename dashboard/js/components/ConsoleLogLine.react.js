import React from 'react';

class ConsoleLogLine extends React.Component {
    render() {
        return (
            <pre>{this.props.string}</pre>
        );
    }
};

export default ConsoleLogLine;
