import React from 'react';
import ControlPanelStore from '../stores/ControlPanelStore';

class ErrorPane extends React.Component {
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
        if(this.state.current_turn + 1 == this.state.max_turns) {
            let messages = this._genMessages();
            
            return (
                <div id="error-pane">
                    {messages}
                </div>
            );
        } else {
            return (
                <div id="error-pane">
                </div>
            );
        }
    }
    
    _genMessages() {
        let result = [];
        
        this.props.match.error_messages.forEach((bot_messages, index1) => {
            let bot_name = '';
            if(bot_messages[0] - 1 == 0) {
                bot_name = this.props.bot1_name;
            } else {
                bot_name = this.props.bot2_name;
            }
            
            result.push(<p key={index1}>Bot {bot_name} has failed for following reason(s):</p>);
            
            bot_messages[1].forEach((message, index2) => {
                let key = `{index1}${index2}`;
                result.push(<p key={key}>{message}</p>);
            });
        });
        
        return result;
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

export default ErrorPane;
