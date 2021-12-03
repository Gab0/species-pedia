// TODO: Find a way to sparate components across multiple files.
class DisplaySpecies extends React.Component {
  constructor(props) {
    super(props);
    this.display = this.display.bind(this);
  }


  displayImage(url) {
    return <img src={url}></img>
  }
  display() {
    try {
      var obj = this.props.readContent().Right;
      //return JSON.stringify(obj)
      return (
        <div>
          <div>
            {obj.remoteResultImages.map(this.displayImage)}
          </div>
        <div>
          {obj.remoteResultWikipedia}
        </div>
        </div>
      )

    } catch (e) {
      console.log('Error ', e)
      return "NOT LOADED"
    };

  }

  render() {
    return (
      <div>
        <p>{this.props.readQuery()}</p>
        <h1>{this.display()}</h1>
      </div>
    );
  }
}


class NameForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {query: ""};
    this.handleChange = this.handleChange.bind(this);
  }

  handleChange(event) {
    // FIXME: Duplicated attributes?
    var k = event.target.value
    this.query = k;
    this.props.updateQuery(k);
    this.setState({query: k});
  }

  render() {
    return (
      <form onSubmit={this.props.handleSubmit}>
        <label>
          Search:
          <input type="text" value={this.state.query} onChange={this.handleChange} />
        </label>
        <input type="submit" value="Submit" />
      </form>
    );
  }
}


class TitleScreen extends React.Component {
  render() {
    return (
      <div id="title" className="centering">
        Species Information Aggregator.
      </div>
    );
  }

}

class MainPage extends React.Component {
  constructor(props) {
    super(props);

    this.state = {query: '', content: {}};
  }

  readContent() {
    return this.state.content;
  }

  readQuery() {
    return this.state.query;
  }

  updateQuery(q) {
    this.state.query=q;
  }

  handleSubmit(event) {
    event.preventDefault();

    var query = this.readQuery();
    console.log("Sending request:", query);

    var data = {
      queryContent: query,
      jsonResponse: false
    }
    console.log(data);
    var request = new XMLHttpRequest();
    request.open('POST', '/search.json', true);
    request.setRequestHeader('Content-Type', 'application/json');
    request.setRequestHeader('Accept', 'application/json');

    request.onload = function (e) {

      var res = JSON.parse(request.responseText);

      console.log("Sending request.");
      console.log(res);

      this.setState({query: this.state.query, content: res});
    }.bind(this)

    var sdata = JSON.stringify(data);
    request.send(sdata);
  }


  render() {
    return <div className="centering">
             <TitleScreen/>
             <NameForm
               handleSubmit={this.handleSubmit.bind(this)}
               updateQuery={this.updateQuery.bind(this)}
             />
             <DisplaySpecies
               readContent={this.readContent.bind(this)}
               readQuery={this.readQuery.bind(this)}
             />
           </div>
  }
}

const mainContainer = document.querySelector('#main');
ReactDOM.render(e(MainPage), mainContainer);
