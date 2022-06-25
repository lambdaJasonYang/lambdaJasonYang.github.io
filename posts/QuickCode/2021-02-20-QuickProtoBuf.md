---
title: Quick ProtoBuf
tags: prog, QuickCode
toc: y
---

# protoc compiler

```{.go filename="helloworld.proto"}
syntax = "proto3";

option go_package = "google.golang.org/grpc/examples/helloworld/helloworld";
option java_multiple_files = true;
option java_package = "io.grpc.examples.helloworld";
option java_outer_classname = "HelloWorldProto";

package helloworld;

// The greeting service definition.
service Greeter {
  // Sends a greeting
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}

// The request message containing the user's name.
message HelloRequest {
  string name = 1;
}

// The response message containing the greetings
message HelloReply {
  string message = 1;
}

```

* In golang protoc will take the `.proto` file to generate a `.pb.go` 
* Golang uses struct composition aka struct embedding which is similar to class inheritance/extension in C++. Note Golang does this because it has no concept of class aka struct inheritance/extension.

```{.go filename="server.go"}
//in Go we embed the protobuf Server into our server struct
type server struct {
	pb.UnimplementedGreeterServer 
}

// SayHello implements helloworld.GreeterServer
func (s *server) SayHello(ctx context.Context, in *pb.HelloRequest) (*pb.HelloReply, error) {
	log.Printf("Received: %v", in.GetName())
	return &pb.HelloReply{Message: "Hello " + in.GetName()}, nil
}

func main() {
	flag.Parse()
	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	pb.RegisterGreeterServer(s, &server{}) //IMPORTANT &server{}
	log.Printf("server listening at %v", lis.Addr())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
```

Observe the similarity between Golang and C++ server grpc implementation.

```{.cpp filename="server.cpp"}
class GreeterServiceImpl final : public Greeter::Service {
  Status SayHello(ServerContext* context, const HelloRequest* request, HelloReply* reply) override {
    std::string prefix("Hello ");
    reply->set_message(prefix + request->name());
    return Status::OK;
  }
};

void main(){
  GreeterServiceImpl service;
  ...
  builder.RegisterService(&service);
  // Finally assemble the server.
}
```