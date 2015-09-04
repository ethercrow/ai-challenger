import React from 'react';
import AppStore from '../stores/AppStore';

import MatchViewerCloseButton from './MatchViewerCloseButton.react';

class MatchViewer extends React.Component {
    constructor(props) {
        super(props);
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        AppStore.onChange(this._onChange);
    }
    
    componentWillUnmount() {
        AppStore.off(this._onChange);
    }
    
    render() {
        return (
            <div className="console-window">
                <header>
                    <div className="console-window-header">{this.state.bot_name1} vs. {this.state.bot_name2}</div>
                    <MatchViewerCloseButton/>
                </header>
            </div>
        );
    }
    
    _resolveState() {
        return {
            match: AppStore.getMatchWindowMatch(),
            bot_name1: AppStore.getBots()[AppStore.getMatchWindowMatch().contesters[0]],
            bot_name2: AppStore.getBots()[AppStore.getMatchWindowMatch().contesters[1]]
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default MatchViewer;
