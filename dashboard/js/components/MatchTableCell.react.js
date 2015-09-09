import React from 'react';
import AppStore from '../stores/AppStore';
import AppActions from '../actions/AppActions';

class MatchTableCell extends React.Component {
    render() {
        if(this.props.label != '-') {
            return (<td className="clickable_cell" onClick={this._onClick.bind(this)}>{this.props.label}</td>);
        } else {
            return (<td>{this.props.label}</td>);
        }
    }
    
    _onClick() {
        if(!AppStore.getDisplayMatchWindow()) {
            AppActions.openMatchWindow(this.props.bot1_index, this.props.bot2_index);
        }
    }
};

export default MatchTableCell;
