import React from 'react';
import AppStore from '../stores/AppStore';

import MatchTableCell from './MatchTableCell.react';

class MatchTable extends React.Component {
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
        let head = this.state.bots.map((bot_name, index) => {
            return (<th key={index}>{bot_name}</th>);
        });
        
        let lines = this.state.table.map((table_line, index) => {
            let results = [];
            for(let i = 0; i < table_line.length; i++) {
                let cell = '';
                if(table_line[i]) {
                    cell = table_line[i]
                } else {
                    cell = '-'
                }
                
                results.push((<MatchTableCell key={i} bot1_index={index} bot2_index={i} label={cell}/>));
            }
            
            return (<tr key={index}><th>{this.state.bots[index]}</th>{results}</tr>);
        });
        
        return (
            <div id="match_table_div">
                <table id="match_table">
                    <caption>RESULTS</caption>
            
                    <thead>
                        <tr><th></th>{head}</tr>
                    </thead>
                    
                    <tbody>
                        {lines}
                    </tbody>
                </table>
            </div>
        );
    }
    
    _resolveState() {
        return {
            bots: AppStore.getBots(),
            table: AppStore.getMatchTable()
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default MatchTable;
