// Import React
import React from "react";

// Import Spectacle Core tags
import {
  BlockQuote,
  Deck,
  Heading,
  Quote,
  Slide,
  Spectacle,
  Text,
  List,
  ListItem,
  Image,
  Appear,
  CodePane,
  S
} from "spectacle";

// Import code slide
import CodeSlide from 'spectacle-code-slide';

import ImageSlide from 'spectacle-image-slide';



// Import theme
import createTheme from "spectacle/lib/themes/default";

require('prismjs/components/prism-ocaml');

// Require CSS
require("normalize.css");
require("spectacle/lib/themes/default/index.css");

const theme = createTheme({
  primary: "#779ecb"
});


const images = {
  adt_tree: require("../assets/adt_tree.png"),
  balanced_tree: require("../assets/balanced_tree.jpg"),
  merkle_tree_account: require("../assets/merkle_tree_account.jpg"),
  merkle_tree_update_h2: require("../assets/merkle_tree_update_h2.jpg"),
  merkle_tree_update_h1: require("../assets/merkle_tree_update_h1.jpg"),
  merkle_tree_update_root: require("../assets/merkle_tree_update_root.jpg"),
  merkle_address_1: require("../assets/merkle_address_1.jpg"),
  merkle_address_2: require("../assets/merkle_address_2.jpg"),
  merkle_tree_h2: require("../assets/merkle_tree_h2.jpg"),
  merkle_tree_h1: require("../assets/merkle_tree_h1.jpg"),
  merkle_tree_root: require("../assets/merkle_tree_root.jpg"),
  merkle_tree_type: require("../assets/merkle_tree_type.jpg")
};

export default class Presentation extends React.Component {
  render() {
    return (
      <Spectacle theme={theme}>
        <Deck transition={["zoom", "slide"]} transitionDuration={500}>
          <Slide transition={['zoom']} bgColor="primary">
            <Heading size={4} caps lineHeight={1} textColor="secondary">
              Designing Secure Cryptographic Data Structures:
          </Heading>
            <Heading size={4} caps lineHeight={1} textColor="secondary">
              A "More" Type-Safe Persistent Merkle Tree
          </Heading>
          <Text margin="10px 0 0" textColor="primary" size={1} bold>
           _
          </Text>
            <Text margin="10px 0 0" textColor="tertiary" size={1} bold>
              John Wu
          </Text>
            <Text margin="10px 0 0" textColor="tertiary" size={1} bold>
              Twitter: @wu_s_john
          </Text>
            <Text margin="10px 0 0" textColor="tertiary" size={1} bold>
              Github: @johnwu93
          </Text>
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <ImageSlide image={images.merkle_tree_account} title="What is a Merkle Tree?" />
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <ImageSlide image={images.merkle_tree_h2} title="Compute the hash of the accounts" />
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <ImageSlide image={images.merkle_tree_h1} title="Compute the hash of the leaves" />
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <ImageSlide image={images.merkle_tree_root} title="Compute the hash of the parent nodes" />
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <ImageSlide image={images.merkle_tree_update_h2} title="Make Updates to an account" />
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <ImageSlide image={images.merkle_tree_update_h1} title="Make Updates to inner node" />
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <ImageSlide image={images.merkle_tree_update_root} title="Make updates until the root is updated" />
          </Slide>
          <Slide transition={["slide", "fade"]} bgColor="primary">
            <Heading size={4} caps lineHeight={1} textColor="tertiary">
              Requirements:
            </Heading>
            <Text textColor="tertiary" size={1}>Create an elegant implementation of a Merkle Tree Persistent Database where all writes are atomic</Text>
          </Slide>
          <CodeSlide
            transition={["slide", "fade"]}
            lang="ocaml"
            code={require("raw-loader!./code/adt.ml")}
            ranges={[
              { loc: [0, 41], title: "Algebraic Data Types (ADT)" },
              { loc: [0, 3] },
              { loc: [4, 9] },
              { loc: [10, 15] },
              { loc: [16, 20], note: "Example" },
              { loc: [10, 15], image: images.adt_tree },
              {loc : [20, 30], note: "Queries with optional results" }
            ]}
          />
          <Slide transition={["fade"]} textColor="primary">
            <Heading size={4} caps lineHeight={1} textColor="tertiary">
              What if we want a Balanced Tree?
            </Heading>
            <Image src={images.balanced_tree}></Image>
          </Slide>
          <CodeSlide
            transition={["slide", "fade"]}
            lang="ocaml"
            code={require("raw-loader!./code/gadt_tree.ml")}
            ranges={[
              { loc: [0, 59], title: "Generalized Algebraic Data Types (GADT)" },
              { loc: [0, 6], note: "'a is restricted to a certain type" },
              { loc: [7, 10], note: "3 is represented at the type level", textColor: "tertiary" },
              { loc: [11, 17], note:"'a is the type of all elements, _ is the constrained to be only natural number types" },
              { loc: [18, 31] },
              { loc: [18, 31], image: images.balanced_tree },
              { loc: [32, 45], note: "total functions without having to match on Leaf" }
            ]}
          />
          <CodeSlide
            transition={["slide", "fade"]}
            lang="ocaml"
            code={require("raw-loader!./code/gadt_vector.ml")}
            ranges={[
              { loc: [0, 23], title: "Vectors with size known at the type level" },
              { loc: [0, 6] },
              { loc: [6, 7] },
              { loc: [13, 18] },
              { loc: [19, 20] },
              { loc: [21, 23], note: "Get Compiling error" },
            ]}
          />
          <CodeSlide
            transition={["slide", "fade"]}
            lang="ocaml"
            code={require("raw-loader!./code/merkle_address.ml")}
            ranges={[
              { loc: [0, 32], title: "Merkle Address: Path of a merkle tree" },
              { loc: [1, 13], note: "These methods are total" },
              { loc: [15, 24], note: "Existential type" },
              { loc: [28, 29] },
              { loc: [28, 29], image: images.merkle_address_1 },
              { loc: [30, 31] },
              { loc: [30, 31], image: images.merkle_address_2 },
              {}
            ]}
          />
          <Slide transition={["slide", "fade"]} bgColor="primary">
            {/* TODO: draw a subgraph surrounding the database */}
            <ImageSlide image={images.merkle_tree_type} title="Let's distinguish account and hash types" />
          </Slide>
          <CodeSlide
            transition={["slide", "fade"]}
            lang="ocaml"
            code={require("raw-loader!./code/merkle_location.ml")}
            ranges={[
              { loc: [0, 12], title: "Merkle Location" },
              { loc: [1, 4] },
            ]}
          />
          <Slide transition={["fade"]} textColor="primary">
            <Appear fid="1">
              <Text margin="10px 0 0" textColor="tertiary" size={1} bold>
                Merkle Tree State
          </Text>
            </Appear>
            <Appear fid="2">
            <List textColor="tertiary" size={4} bold>
              <ListItem>Accounts</ListItem>
              <ListItem>Addresses to Accounts</ListItem>
              <ListItem>Hashes and their Address</ListItem>
              <ListItem>Next Allocated Space</ListItem>
            </List>
            </Appear>
            <Appear fid="3">
              <Text margin="10px 0 0" textColor="tertiary" size={1} bold>
                We can store all of this in a Key-Value Database
          </Text>
            </Appear>
          </Slide>
          <CodeSlide
            transition={["slide", "fade"]}
            lang="ocaml"
            code={require("raw-loader!./code/database_lookup.ml")}
            ranges={[
              { loc: [0, 32], title: "Our database stores string key-value pairs" },
              { loc: [1, 6], note:"Serialize and deserialize heterogenous key-value pairs" },
              { loc: [13, 23] },
              { loc: [25, 29] }
            ]}
          />
          <CodeSlide
            transition={["slide", "fade"]}
            lang="ocaml"
            code={require("raw-loader!./code/get_set_database.ml")}
            ranges={[
              { loc: [0, 20], title: "Getters and Setters" },
              { loc: [0, 7], note: "Flexible method for obtaining data for a particular key-value pair" },
              { loc: [9, 10], note: "Get an account path via public key" },
              { loc: [11, 12], note: "Get an account via account path" },
              { loc: [13, 20] },
            ]}
          />
          <Slide>
            <CodePane source="CodePane" lang="ocaml" source={require("raw-loader!./code/merkle_database_methods.ml")} />
          </Slide>
          <Slide transition={["fade"]} bgColor="secondary" textColor="primary">
            <BlockQuote>
              <Quote>
                <em>"Fin"</em>
              </Quote>
            </BlockQuote>
          </Slide>
        </Deck>
      </Spectacle>
    );
  }
}
