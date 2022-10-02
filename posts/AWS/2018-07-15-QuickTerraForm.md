---
title: Quick TerraForm
tags: prog, cloud, Terraform, DevOps, aws
---

TerraForm is declarative, apply is idempotent

```yml

terraform {
    required_providers {
        aws = {
            source = "hashicorp/aws"
            version = "~> 3.0"
        }
    }
}

provider "aws" {
    region = "us-east-1"
}

resource "aws_instance" "example" {
    ami = "ami-05fa00d4c63e32376"
    instance_type = "t2.micro"
}
```



```bash
terraform init #downloads the terraform modules
terraform plan #query aws API and see what will be deployed and its settings
terraform apply #starts the instance in aws
```

```bash
terraform destroy
```

# keywords

* `resource "aws_instance" "example"` : creates a aws_instance if not exist
* `data "aws_vpc" "default_vpc"` : refers to pre-existing aws_vpc 

# Theory

* State file : has all the secrets and 
  * can keep it in s3


```.txt
├── main.tf
├── .terraform
│   └── providers
│       └── registry.terraform.io
│           └── hashicorp
│               └── aws
│                   └── 3.75.2
│                       └── linux_amd64
│                           └── terraform-provider-aws_v3.75.2_x5
├── .terraform.lock.hcl
├── terraform.tfstate
└── terraform.tfstate.backup
```