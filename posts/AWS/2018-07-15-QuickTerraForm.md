---
title: Quick TerraForm
tags: QuickCode, cloud, DevOps
---

TerraForm is declarative, apply is idempotent

```text
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
    ami = "ami-01189924"
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