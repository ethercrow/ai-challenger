import React from 'react';
import AppStore from '../stores/AppStore';
import DrawMatch from '../utils/DrawMatch';

import MatchViewerCloseButton from './MatchViewerCloseButton.react';

class MatchViewer extends React.Component {
    constructor(props) {
        super(props);
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        AppStore.onChange(this._onChange);
        
        this.state.renderer = this._createRenderer();
        this.state.renderer.start();
    }
    
    componentWillUnmount() {
        AppStore.off(this._onChange);
        this.state.renderer.stop();
    }
    
    render() {
        return (
            <div className="console-window">
                <header>
                    <div className="console-window-header">{this.state.bot_name1} vs. {this.state.bot_name2}</div>
                    <MatchViewerCloseButton/>
                </header>
            
                <canvas id="match-view" width="800" height="600">
                    Sorry, you browser doesn&#39;t support HTML5 canvas API!
                </canvas>
            </div>
        );
    }
    
    _resolveState() {
        if(AppStore.getDisplayMatchWindow()) {
            return {
                renderer: undefined,
            
                match: AppStore.getMatchWindowMatch(),
                bot_name1: AppStore.getBots()[AppStore.getMatchWindowMatch().contesters[0]],
                bot_name2: AppStore.getBots()[AppStore.getMatchWindowMatch().contesters[1]]
            };
        } else {
            return {
                rendere: undefined,
                
                match: undefined,
                bot_name1: undefined,
                bot_name2: undefined
            }
        }
    }
    
    _createRenderer() {
        let canvas = document.getElementById('match-view');
        return new DrawMatch(canvas, this.state.match, this.state.bot_name1, this.state.bot_name2);
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default MatchViewer;
