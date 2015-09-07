import React from 'react';
import ControlPanelStore from '../stores/ControlPanelStore';

class TurnsCounter extends React.Component {
    constructor(props) {
        super(props);
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        ControlPanelStore.onChange(this._onChange);
    }
    
    componentWillUnmount() {
        ControlPanelStore.off(this._onChange);
    }
    
    render() {
        return (
            <div id="turns-counter">
                <p id="turns-counter-text">{this.state.current_turn + 1}/{this.state.max_turns}</p>
            </div>
        );
    }
    
    _resolveState() {
        return {
            current_turn: ControlPanelStore.getCurrentTurn(),
            max_turns: ControlPanelStore.getMaxTurns()
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default TurnsCounter;
