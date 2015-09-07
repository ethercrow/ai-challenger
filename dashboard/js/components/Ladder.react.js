import React from 'react';

import AppStore from '../stores/AppStore';

import LadderLine from './LadderLine.react';

class Ladder extends React.Component {
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
        var lines = this.state.data.map((bot, index) => {
            return (
                <LadderLine key={index} rank={bot.rank} name={bot.name} win={bot.win} loose={bot.lost}/>
            );
        });
        
        return (
            <div id="left_column">
                <table id="ladder">
                    <caption>LADDER</caption>

                    <thead>
                        <tr><th></th><th>Name</th><th>Win/Lost</th></tr>
                    </thead>

                    <tbody>
                        {lines}
                    </tbody>
                </table>
            </div>
        );
    }
    
    _resolveState() {
        let new_data = [];
        AppStore.getBots().forEach((bot_name, bot_index) => {
            new_data.push({
                name: bot_name,
                rank: 0,
                win: AppStore.getBotNumVictories(bot_index), 
                lost: AppStore.getBotNumDefeats(bot_index)
            });
        });
        
        new_data.sort((a, b) => {
                if(a.win < b.win) return 1;
                if(a.win > b.win) return -1;
                return 0;
        });
        
        let cur_rank = 1;
        new_data.forEach((bot, index) => {
            if(index == 0 || bot.win == new_data[index - 1].win) {
                new_data[index].rank = cur_rank;
            } else {
                new_data[index].rank = ++cur_rank;
            }
        });
        
        return { 
            data: new_data.filter((bot) => { return bot.rank <= 10; })
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default Ladder;
