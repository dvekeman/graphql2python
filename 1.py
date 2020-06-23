from __future__ import annotations
import datetime
import decimal
from dataclasses import dataclass
from typing import Optional, Dict, List
from enum import Enum, unique
from uuid import UUID

@dataclass(frozen=True)
class customer:
    created_at: datetime.datetime
    custom_fields: Dict
    customer_id: str
    invoices: List[invoice]
    invoices_aggregate: invoice_aggregate
    reference: UUID
    updated_at: datetime.datetime    

    def to_json(self):
        res = {}
        if self.created_at is not None:
            res["created_at"] = self.created_at
        if self.custom_fields is not None:
            res["custom_fields"] = self.custom_fields
        if self.customer_id is not None:
            res["customer_id"] = self.customer_id
        if self.invoices is not None:
            res["invoices"] = self.invoices
        if self.invoices_aggregate is not None:
            res["invoices_aggregate"] = self.invoices_aggregate
        if self.reference is not None:
            res["reference"] = self.reference
        if self.updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = ("created_at" in d) if d["created_at"] else None,
            custom_fields = ("custom_fields" in d) if d["custom_fields"] else None,
            customer_id = ("customer_id" in d) if d["customer_id"] else None,
            invoices = ("invoices" in d) if d["invoices"] else None,
            invoices_aggregate = ("invoices_aggregate" in d) if d["invoices_aggregate"] else None,
            reference = ("reference" in d) if d["reference"] else None,
            updated_at = ("updated_at" in d) if d["updated_at"] else None,
        )
        pass

@dataclass(frozen=True)
class customer_aggregate:
    nodes: List[customer]
    aggregate: Optional[customer_aggregate_fields] = None    

    def to_json(self):
        res = {}
        if self.aggregate is not None:
            res["aggregate"] = self.aggregate
        if self.nodes is not None:
            res["nodes"] = self.nodes
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            aggregate = ("aggregate" in d) if d["aggregate"] else None,
            nodes = ("nodes" in d) if d["nodes"] else None,
        )
        pass

@dataclass(frozen=True)
class customer_aggregate_fields:
    count: Optional[int] = None
    max: Optional[customer_max_fields] = None
    min: Optional[customer_min_fields] = None    

    def to_json(self):
        res = {}
        if self.count is not None:
            res["count"] = self.count
        if self.max is not None:
            res["max"] = self.max
        if self.min is not None:
            res["min"] = self.min
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            count = ("count" in d) if d["count"] else None,
            max = ("max" in d) if d["max"] else None,
            min = ("min" in d) if d["min"] else None,
        )
        pass

@unique
class customer_constraint(Enum):
    customer_pkey= "customer_pkey"

@dataclass(frozen=True)
class customer_max_fields:
    created_at: Optional[datetime.datetime] = None
    customer_id: Optional[str] = None
    reference: Optional[UUID] = None
    updated_at: Optional[datetime.datetime] = None    

    def to_json(self):
        res = {}
        if self.created_at is not None:
            res["created_at"] = self.created_at
        if self.customer_id is not None:
            res["customer_id"] = self.customer_id
        if self.reference is not None:
            res["reference"] = self.reference
        if self.updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = ("created_at" in d) if d["created_at"] else None,
            customer_id = ("customer_id" in d) if d["customer_id"] else None,
            reference = ("reference" in d) if d["reference"] else None,
            updated_at = ("updated_at" in d) if d["updated_at"] else None,
        )
        pass

@dataclass(frozen=True)
class customer_min_fields:
    created_at: Optional[datetime.datetime] = None
    customer_id: Optional[str] = None
    reference: Optional[UUID] = None
    updated_at: Optional[datetime.datetime] = None    

    def to_json(self):
        res = {}
        if self.created_at is not None:
            res["created_at"] = self.created_at
        if self.customer_id is not None:
            res["customer_id"] = self.customer_id
        if self.reference is not None:
            res["reference"] = self.reference
        if self.updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = ("created_at" in d) if d["created_at"] else None,
            customer_id = ("customer_id" in d) if d["customer_id"] else None,
            reference = ("reference" in d) if d["reference"] else None,
            updated_at = ("updated_at" in d) if d["updated_at"] else None,
        )
        pass

@dataclass(frozen=True)
class customer_mutation_response:
    affected_rows: int
    returning: List[customer]    

    def to_json(self):
        res = {}
        if self.affected_rows is not None:
            res["affected_rows"] = self.affected_rows
        if self.returning is not None:
            res["returning"] = self.returning
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            affected_rows = ("affected_rows" in d) if d["affected_rows"] else None,
            returning = ("returning" in d) if d["returning"] else None,
        )
        pass

@unique
class customer_select_column(Enum):
    created_at= "created_at"
    custom_fields= "custom_fields"
    customer_id= "customer_id"
    reference= "reference"
    updated_at= "updated_at"

@unique
class customer_update_column(Enum):
    created_at= "created_at"
    custom_fields= "custom_fields"
    customer_id= "customer_id"
    reference= "reference"
    updated_at= "updated_at"

@dataclass(frozen=True)
class invoice:
    created_at: datetime.datetime
    customer: UUID
    customer_by_reference: customer
    due_date: datetime.datetime
    invoice_number: str
    processing_status: str
    reference: UUID
    total_amount: decimal.Decimal
    total_amount_currency: str
    updated_at: datetime.datetime
    status: Optional[str] = None    

    def to_json(self):
        res = {}
        if self.created_at is not None:
            res["created_at"] = self.created_at
        if self.customer is not None:
            res["customer"] = self.customer
        if self.customer_by_reference is not None:
            res["customer_by_reference"] = self.customer_by_reference
        if self.due_date is not None:
            res["due_date"] = self.due_date
        if self.invoice_number is not None:
            res["invoice_number"] = self.invoice_number
        if self.processing_status is not None:
            res["processing_status"] = self.processing_status
        if self.reference is not None:
            res["reference"] = self.reference
        if self.status is not None:
            res["status"] = self.status
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        if self.total_amount_currency is not None:
            res["total_amount_currency"] = self.total_amount_currency
        if self.updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = ("created_at" in d) if d["created_at"] else None,
            customer = ("customer" in d) if d["customer"] else None,
            customer_by_reference = ("customer_by_reference" in d) if d["customer_by_reference"] else None,
            due_date = ("due_date" in d) if d["due_date"] else None,
            invoice_number = ("invoice_number" in d) if d["invoice_number"] else None,
            processing_status = ("processing_status" in d) if d["processing_status"] else None,
            reference = ("reference" in d) if d["reference"] else None,
            status = ("status" in d) if d["status"] else None,
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
            total_amount_currency = ("total_amount_currency" in d) if d["total_amount_currency"] else None,
            updated_at = ("updated_at" in d) if d["updated_at"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_aggregate:
    nodes: List[invoice]
    aggregate: Optional[invoice_aggregate_fields] = None    

    def to_json(self):
        res = {}
        if self.aggregate is not None:
            res["aggregate"] = self.aggregate
        if self.nodes is not None:
            res["nodes"] = self.nodes
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            aggregate = ("aggregate" in d) if d["aggregate"] else None,
            nodes = ("nodes" in d) if d["nodes"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_aggregate_fields:
    avg: Optional[invoice_avg_fields] = None
    count: Optional[int] = None
    max: Optional[invoice_max_fields] = None
    min: Optional[invoice_min_fields] = None
    stddev: Optional[invoice_stddev_fields] = None
    stddev_pop: Optional[invoice_stddev_pop_fields] = None
    stddev_samp: Optional[invoice_stddev_samp_fields] = None
    sum: Optional[invoice_sum_fields] = None
    var_pop: Optional[invoice_var_pop_fields] = None
    var_samp: Optional[invoice_var_samp_fields] = None
    variance: Optional[invoice_variance_fields] = None    

    def to_json(self):
        res = {}
        if self.avg is not None:
            res["avg"] = self.avg
        if self.count is not None:
            res["count"] = self.count
        if self.max is not None:
            res["max"] = self.max
        if self.min is not None:
            res["min"] = self.min
        if self.stddev is not None:
            res["stddev"] = self.stddev
        if self.stddev_pop is not None:
            res["stddev_pop"] = self.stddev_pop
        if self.stddev_samp is not None:
            res["stddev_samp"] = self.stddev_samp
        if self.sum is not None:
            res["sum"] = self.sum
        if self.var_pop is not None:
            res["var_pop"] = self.var_pop
        if self.var_samp is not None:
            res["var_samp"] = self.var_samp
        if self.variance is not None:
            res["variance"] = self.variance
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            avg = ("avg" in d) if d["avg"] else None,
            count = ("count" in d) if d["count"] else None,
            max = ("max" in d) if d["max"] else None,
            min = ("min" in d) if d["min"] else None,
            stddev = ("stddev" in d) if d["stddev"] else None,
            stddev_pop = ("stddev_pop" in d) if d["stddev_pop"] else None,
            stddev_samp = ("stddev_samp" in d) if d["stddev_samp"] else None,
            sum = ("sum" in d) if d["sum"] else None,
            var_pop = ("var_pop" in d) if d["var_pop"] else None,
            var_samp = ("var_samp" in d) if d["var_samp"] else None,
            variance = ("variance" in d) if d["variance"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_avg_fields:
    total_amount: Optional[float] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@unique
class invoice_constraint(Enum):
    invoice_pkey= "invoice_pkey"

@dataclass(frozen=True)
class invoice_max_fields:
    created_at: Optional[datetime.datetime] = None
    customer: Optional[UUID] = None
    due_date: Optional[datetime.datetime] = None
    invoice_number: Optional[str] = None
    processing_status: Optional[str] = None
    reference: Optional[UUID] = None
    total_amount: Optional[decimal.Decimal] = None
    total_amount_currency: Optional[str] = None
    updated_at: Optional[datetime.datetime] = None    

    def to_json(self):
        res = {}
        if self.created_at is not None:
            res["created_at"] = self.created_at
        if self.customer is not None:
            res["customer"] = self.customer
        if self.due_date is not None:
            res["due_date"] = self.due_date
        if self.invoice_number is not None:
            res["invoice_number"] = self.invoice_number
        if self.processing_status is not None:
            res["processing_status"] = self.processing_status
        if self.reference is not None:
            res["reference"] = self.reference
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        if self.total_amount_currency is not None:
            res["total_amount_currency"] = self.total_amount_currency
        if self.updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = ("created_at" in d) if d["created_at"] else None,
            customer = ("customer" in d) if d["customer"] else None,
            due_date = ("due_date" in d) if d["due_date"] else None,
            invoice_number = ("invoice_number" in d) if d["invoice_number"] else None,
            processing_status = ("processing_status" in d) if d["processing_status"] else None,
            reference = ("reference" in d) if d["reference"] else None,
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
            total_amount_currency = ("total_amount_currency" in d) if d["total_amount_currency"] else None,
            updated_at = ("updated_at" in d) if d["updated_at"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_min_fields:
    created_at: Optional[datetime.datetime] = None
    customer: Optional[UUID] = None
    due_date: Optional[datetime.datetime] = None
    invoice_number: Optional[str] = None
    processing_status: Optional[str] = None
    reference: Optional[UUID] = None
    total_amount: Optional[decimal.Decimal] = None
    total_amount_currency: Optional[str] = None
    updated_at: Optional[datetime.datetime] = None    

    def to_json(self):
        res = {}
        if self.created_at is not None:
            res["created_at"] = self.created_at
        if self.customer is not None:
            res["customer"] = self.customer
        if self.due_date is not None:
            res["due_date"] = self.due_date
        if self.invoice_number is not None:
            res["invoice_number"] = self.invoice_number
        if self.processing_status is not None:
            res["processing_status"] = self.processing_status
        if self.reference is not None:
            res["reference"] = self.reference
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        if self.total_amount_currency is not None:
            res["total_amount_currency"] = self.total_amount_currency
        if self.updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = ("created_at" in d) if d["created_at"] else None,
            customer = ("customer" in d) if d["customer"] else None,
            due_date = ("due_date" in d) if d["due_date"] else None,
            invoice_number = ("invoice_number" in d) if d["invoice_number"] else None,
            processing_status = ("processing_status" in d) if d["processing_status"] else None,
            reference = ("reference" in d) if d["reference"] else None,
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
            total_amount_currency = ("total_amount_currency" in d) if d["total_amount_currency"] else None,
            updated_at = ("updated_at" in d) if d["updated_at"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_mutation_response:
    affected_rows: int
    returning: List[invoice]    

    def to_json(self):
        res = {}
        if self.affected_rows is not None:
            res["affected_rows"] = self.affected_rows
        if self.returning is not None:
            res["returning"] = self.returning
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            affected_rows = ("affected_rows" in d) if d["affected_rows"] else None,
            returning = ("returning" in d) if d["returning"] else None,
        )
        pass

@unique
class invoice_select_column(Enum):
    created_at= "created_at"
    customer= "customer"
    due_date= "due_date"
    invoice_number= "invoice_number"
    processing_status= "processing_status"
    reference= "reference"
    total_amount= "total_amount"
    total_amount_currency= "total_amount_currency"
    updated_at= "updated_at"

@dataclass(frozen=True)
class invoice_stddev_fields:
    total_amount: Optional[float] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_stddev_pop_fields:
    total_amount: Optional[float] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_stddev_samp_fields:
    total_amount: Optional[float] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_sum_fields:
    total_amount: Optional[decimal.Decimal] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@unique
class invoice_update_column(Enum):
    created_at= "created_at"
    customer= "customer"
    due_date= "due_date"
    invoice_number= "invoice_number"
    processing_status= "processing_status"
    reference= "reference"
    total_amount= "total_amount"
    total_amount_currency= "total_amount_currency"
    updated_at= "updated_at"

@dataclass(frozen=True)
class invoice_var_pop_fields:
    total_amount: Optional[float] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_var_samp_fields:
    total_amount: Optional[float] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@dataclass(frozen=True)
class invoice_variance_fields:
    total_amount: Optional[float] = None    

    def to_json(self):
        res = {}
        if self.total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = ("total_amount" in d) if d["total_amount"] else None,
        )
        pass

@dataclass(frozen=True)
class mutation_root:
    delete_customer: Optional[customer_mutation_response] = None
    delete_customer_by_pk: Optional[customer] = None
    delete_invoice: Optional[invoice_mutation_response] = None
    delete_invoice_by_pk: Optional[invoice] = None
    insert_customer: Optional[customer_mutation_response] = None
    insert_customer_one: Optional[customer] = None
    insert_invoice: Optional[invoice_mutation_response] = None
    insert_invoice_one: Optional[invoice] = None
    update_customer: Optional[customer_mutation_response] = None
    update_customer_by_pk: Optional[customer] = None
    update_invoice: Optional[invoice_mutation_response] = None
    update_invoice_by_pk: Optional[invoice] = None    

    def to_json(self):
        res = {}
        if self.delete_customer is not None:
            res["delete_customer"] = self.delete_customer
        if self.delete_customer_by_pk is not None:
            res["delete_customer_by_pk"] = self.delete_customer_by_pk
        if self.delete_invoice is not None:
            res["delete_invoice"] = self.delete_invoice
        if self.delete_invoice_by_pk is not None:
            res["delete_invoice_by_pk"] = self.delete_invoice_by_pk
        if self.insert_customer is not None:
            res["insert_customer"] = self.insert_customer
        if self.insert_customer_one is not None:
            res["insert_customer_one"] = self.insert_customer_one
        if self.insert_invoice is not None:
            res["insert_invoice"] = self.insert_invoice
        if self.insert_invoice_one is not None:
            res["insert_invoice_one"] = self.insert_invoice_one
        if self.update_customer is not None:
            res["update_customer"] = self.update_customer
        if self.update_customer_by_pk is not None:
            res["update_customer_by_pk"] = self.update_customer_by_pk
        if self.update_invoice is not None:
            res["update_invoice"] = self.update_invoice
        if self.update_invoice_by_pk is not None:
            res["update_invoice_by_pk"] = self.update_invoice_by_pk
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            delete_customer = ("delete_customer" in d) if d["delete_customer"] else None,
            delete_customer_by_pk = ("delete_customer_by_pk" in d) if d["delete_customer_by_pk"] else None,
            delete_invoice = ("delete_invoice" in d) if d["delete_invoice"] else None,
            delete_invoice_by_pk = ("delete_invoice_by_pk" in d) if d["delete_invoice_by_pk"] else None,
            insert_customer = ("insert_customer" in d) if d["insert_customer"] else None,
            insert_customer_one = ("insert_customer_one" in d) if d["insert_customer_one"] else None,
            insert_invoice = ("insert_invoice" in d) if d["insert_invoice"] else None,
            insert_invoice_one = ("insert_invoice_one" in d) if d["insert_invoice_one"] else None,
            update_customer = ("update_customer" in d) if d["update_customer"] else None,
            update_customer_by_pk = ("update_customer_by_pk" in d) if d["update_customer_by_pk"] else None,
            update_invoice = ("update_invoice" in d) if d["update_invoice"] else None,
            update_invoice_by_pk = ("update_invoice_by_pk" in d) if d["update_invoice_by_pk"] else None,
        )
        pass

@unique
class order_by(Enum):
    asc= "asc"
    asc_nulls_first= "asc_nulls_first"
    asc_nulls_last= "asc_nulls_last"
    desc= "desc"
    desc_nulls_first= "desc_nulls_first"
    desc_nulls_last= "desc_nulls_last"

@dataclass(frozen=True)
class query_root:
    customer: List[customer]
    customer_aggregate: customer_aggregate
    invoice: List[invoice]
    invoice_aggregate: invoice_aggregate
    customer_by_pk: Optional[customer] = None
    invoice_by_pk: Optional[invoice] = None    

    def to_json(self):
        res = {}
        if self.customer is not None:
            res["customer"] = self.customer
        if self.customer_aggregate is not None:
            res["customer_aggregate"] = self.customer_aggregate
        if self.customer_by_pk is not None:
            res["customer_by_pk"] = self.customer_by_pk
        if self.invoice is not None:
            res["invoice"] = self.invoice
        if self.invoice_aggregate is not None:
            res["invoice_aggregate"] = self.invoice_aggregate
        if self.invoice_by_pk is not None:
            res["invoice_by_pk"] = self.invoice_by_pk
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            customer = ("customer" in d) if d["customer"] else None,
            customer_aggregate = ("customer_aggregate" in d) if d["customer_aggregate"] else None,
            customer_by_pk = ("customer_by_pk" in d) if d["customer_by_pk"] else None,
            invoice = ("invoice" in d) if d["invoice"] else None,
            invoice_aggregate = ("invoice_aggregate" in d) if d["invoice_aggregate"] else None,
            invoice_by_pk = ("invoice_by_pk" in d) if d["invoice_by_pk"] else None,
        )
        pass

@dataclass(frozen=True)
class subscription_root:
    customer: List[customer]
    customer_aggregate: customer_aggregate
    invoice: List[invoice]
    invoice_aggregate: invoice_aggregate
    customer_by_pk: Optional[customer] = None
    invoice_by_pk: Optional[invoice] = None    

    def to_json(self):
        res = {}
        if self.customer is not None:
            res["customer"] = self.customer
        if self.customer_aggregate is not None:
            res["customer_aggregate"] = self.customer_aggregate
        if self.customer_by_pk is not None:
            res["customer_by_pk"] = self.customer_by_pk
        if self.invoice is not None:
            res["invoice"] = self.invoice
        if self.invoice_aggregate is not None:
            res["invoice_aggregate"] = self.invoice_aggregate
        if self.invoice_by_pk is not None:
            res["invoice_by_pk"] = self.invoice_by_pk
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            customer = ("customer" in d) if d["customer"] else None,
            customer_aggregate = ("customer_aggregate" in d) if d["customer_aggregate"] else None,
            customer_by_pk = ("customer_by_pk" in d) if d["customer_by_pk"] else None,
            invoice = ("invoice" in d) if d["invoice"] else None,
            invoice_aggregate = ("invoice_aggregate" in d) if d["invoice_aggregate"] else None,
            invoice_by_pk = ("invoice_by_pk" in d) if d["invoice_by_pk"] else None,
        )
        pass


