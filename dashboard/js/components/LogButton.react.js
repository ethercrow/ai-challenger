import React from 'react';

class LogButton extends React.Component {
    render() {
        return (
            <div id="log-button">
                <p><a href={this.props.log_url}>Log</a></p>
            </div>
        );
    }
};

export default LogButton;
