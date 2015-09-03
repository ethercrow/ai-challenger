import React from 'react/addons';

import AppStore from '../stores/AppStore';

import Header from './Header.react';
import Ladder from './Ladder.react';
import Console from './Console.react';
import MatchViewer from './MatchViewer.react';

let ReactCSSTransitionGroup = React.addons.CSSTransitionGroup;

class AIChallengerApp extends React.Component {
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
        let win = this.state.display_match_window?(<MatchViewer key="1"/>):[];
        
        return (
            <div>
                <Header/>
                <Ladder/>
                <Console/>
            
                <ReactCSSTransitionGroup transitionName="console-window">
                    {win}
                </ReactCSSTransitionGroup>
            </div>
        );
    }
    
    _resolveState() {
        return {
            display_match_window: AppStore.getDisplayMatchWindow()
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default AIChallengerApp;
