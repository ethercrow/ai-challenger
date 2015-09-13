import React from 'react';
import AppStore from '../stores/AppStore';

import MatchViewerCloseButton from './MatchViewerCloseButton.react';
import GridView from './GridView.react';
import ViewControlPanel from './ViewControlPanel.react';

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
            
                <GridView match={this.state.match} bot1_name={this.state.bot_name1} bot2_name={this.state.bot_name2}/>
            
                <footer>
                    <ViewControlPanel/>
                </footer>
            </div>
        );
    }
    
    _resolveState() {
        if(AppStore.getDisplayMatchWindow()) {
            return {
                match: AppStore.getMatchWindowMatch(),
                bot_name1: AppStore.getBots()[AppStore.getMatchWindowMatch().contesters[0]],
                bot_name2: AppStore.getBots()[AppStore.getMatchWindowMatch().contesters[1]]
            };
        } else {
            return {
                match: undefined,
                bot_name1: '',
                bot_name2: ''
            };
        }
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default MatchViewer;
