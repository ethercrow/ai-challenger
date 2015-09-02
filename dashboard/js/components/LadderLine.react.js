import React from 'react';

class LadderLine extends React.Component {
    render() {
        return (
            <tr><td>{this.props.rank}</td><td>{this.props.name}</td><td>{this.props.win}/{this.props.loose}</td></tr>
        );
    }
};

export default LadderLine;
