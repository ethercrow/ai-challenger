import React from 'react';
import AppActions from '../actions/AppActions';

class MatchViewerCloseButton extends React.Component {
    render() {
        return (
            <div className="console-window-close-button" onClick={this._onClick}>X</div>
        );
    }
    
    _onClick() {
        AppActions.closeMatchWindow();
    }
};

export default MatchViewerCloseButton;
