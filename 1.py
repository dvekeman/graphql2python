from __future__ import annotations
import datetime
import decimal
from dataclasses import dataclass
from typing import Optional, Dict, List
from enum import Enum, unique
from uuid import UUID

@dataclass(frozen=True)
class customer:
    created_at: Optional[datetime.datetime] = None
    custom_fields: Optional[Dict] = None
    customer_id: Optional[str] = None
    invoices: Optional[List[invoice]] = None
    invoices_aggregate: Optional[invoice_aggregate] = None
    reference: Optional[UUID] = None
    updated_at: Optional[datetime.datetime] = None    

    def to_json(self):
        res = {}
        if created_at is not None:
            res["created_at"] = self.created_at
        if custom_fields is not None:
            res["custom_fields"] = self.custom_fields
        if customer_id is not None:
            res["customer_id"] = self.customer_id
        if invoices is not None:
            res["invoices"] = self.invoices
        if invoices_aggregate is not None:
            res["invoices_aggregate"] = self.invoices_aggregate
        if reference is not None:
            res["reference"] = self.reference
        if updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = d["created_at"],
            custom_fields = d["custom_fields"],
            customer_id = d["customer_id"],
            invoices = d["invoices"],
            invoices_aggregate = d["invoices_aggregate"],
            reference = d["reference"],
            updated_at = d["updated_at"],
        )
        pass

@dataclass(frozen=True)
class customer_aggregate:
    aggregate: customer_aggregate_fields
    nodes: Optional[List[customer]] = None    

    def to_json(self):
        res = {}
        if aggregate is not None:
            res["aggregate"] = self.aggregate
        if nodes is not None:
            res["nodes"] = self.nodes
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            aggregate = d["aggregate"],
            nodes = d["nodes"],
        )
        pass

@dataclass(frozen=True)
class customer_aggregate_fields:
    count: int
    max: customer_max_fields
    min: customer_min_fields    

    def to_json(self):
        res = {}
        if count is not None:
            res["count"] = self.count
        if max is not None:
            res["max"] = self.max
        if min is not None:
            res["min"] = self.min
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            count = d["count"],
            max = d["max"],
            min = d["min"],
        )
        pass

@unique
class customer_constraint(Enum):
    customer_pkey= "customer_pkey"

@dataclass(frozen=True)
class customer_max_fields:
    created_at: datetime.datetime
    customer_id: str
    reference: UUID
    updated_at: datetime.datetime    

    def to_json(self):
        res = {}
        if created_at is not None:
            res["created_at"] = self.created_at
        if customer_id is not None:
            res["customer_id"] = self.customer_id
        if reference is not None:
            res["reference"] = self.reference
        if updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = d["created_at"],
            customer_id = d["customer_id"],
            reference = d["reference"],
            updated_at = d["updated_at"],
        )
        pass

@dataclass(frozen=True)
class customer_min_fields:
    created_at: datetime.datetime
    customer_id: str
    reference: UUID
    updated_at: datetime.datetime    

    def to_json(self):
        res = {}
        if created_at is not None:
            res["created_at"] = self.created_at
        if customer_id is not None:
            res["customer_id"] = self.customer_id
        if reference is not None:
            res["reference"] = self.reference
        if updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = d["created_at"],
            customer_id = d["customer_id"],
            reference = d["reference"],
            updated_at = d["updated_at"],
        )
        pass

@dataclass(frozen=True)
class customer_mutation_response:
    affected_rows: Optional[int] = None
    returning: Optional[List[customer]] = None    

    def to_json(self):
        res = {}
        if affected_rows is not None:
            res["affected_rows"] = self.affected_rows
        if returning is not None:
            res["returning"] = self.returning
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            affected_rows = d["affected_rows"],
            returning = d["returning"],
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
    status: str
    created_at: Optional[datetime.datetime] = None
    customer: Optional[UUID] = None
    customer_by_reference: Optional[customer] = None
    due_date: Optional[datetime.datetime] = None
    invoice_number: Optional[str] = None
    processing_status: Optional[str] = None
    reference: Optional[UUID] = None
    total_amount: Optional[int] = None
    total_amount_currency: Optional[str] = None
    updated_at: Optional[datetime.datetime] = None    

    def to_json(self):
        res = {}
        if created_at is not None:
            res["created_at"] = self.created_at
        if customer is not None:
            res["customer"] = self.customer
        if customer_by_reference is not None:
            res["customer_by_reference"] = self.customer_by_reference
        if due_date is not None:
            res["due_date"] = self.due_date
        if invoice_number is not None:
            res["invoice_number"] = self.invoice_number
        if processing_status is not None:
            res["processing_status"] = self.processing_status
        if reference is not None:
            res["reference"] = self.reference
        if status is not None:
            res["status"] = self.status
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        if total_amount_currency is not None:
            res["total_amount_currency"] = self.total_amount_currency
        if updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = d["created_at"],
            customer = d["customer"],
            customer_by_reference = d["customer_by_reference"],
            due_date = d["due_date"],
            invoice_number = d["invoice_number"],
            processing_status = d["processing_status"],
            reference = d["reference"],
            status = d["status"],
            total_amount = d["total_amount"],
            total_amount_currency = d["total_amount_currency"],
            updated_at = d["updated_at"],
        )
        pass

@dataclass(frozen=True)
class invoice_aggregate:
    aggregate: invoice_aggregate_fields
    nodes: Optional[List[invoice]] = None    

    def to_json(self):
        res = {}
        if aggregate is not None:
            res["aggregate"] = self.aggregate
        if nodes is not None:
            res["nodes"] = self.nodes
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            aggregate = d["aggregate"],
            nodes = d["nodes"],
        )
        pass

@dataclass(frozen=True)
class invoice_aggregate_fields:
    avg: invoice_avg_fields
    count: int
    max: invoice_max_fields
    min: invoice_min_fields
    stddev: invoice_stddev_fields
    stddev_pop: invoice_stddev_pop_fields
    stddev_samp: invoice_stddev_samp_fields
    sum: invoice_sum_fields
    var_pop: invoice_var_pop_fields
    var_samp: invoice_var_samp_fields
    variance: invoice_variance_fields    

    def to_json(self):
        res = {}
        if avg is not None:
            res["avg"] = self.avg
        if count is not None:
            res["count"] = self.count
        if max is not None:
            res["max"] = self.max
        if min is not None:
            res["min"] = self.min
        if stddev is not None:
            res["stddev"] = self.stddev
        if stddev_pop is not None:
            res["stddev_pop"] = self.stddev_pop
        if stddev_samp is not None:
            res["stddev_samp"] = self.stddev_samp
        if sum is not None:
            res["sum"] = self.sum
        if var_pop is not None:
            res["var_pop"] = self.var_pop
        if var_samp is not None:
            res["var_samp"] = self.var_samp
        if variance is not None:
            res["variance"] = self.variance
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            avg = d["avg"],
            count = d["count"],
            max = d["max"],
            min = d["min"],
            stddev = d["stddev"],
            stddev_pop = d["stddev_pop"],
            stddev_samp = d["stddev_samp"],
            sum = d["sum"],
            var_pop = d["var_pop"],
            var_samp = d["var_samp"],
            variance = d["variance"],
        )
        pass

@dataclass(frozen=True)
class invoice_avg_fields:
    total_amount: float    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
        )
        pass

@unique
class invoice_constraint(Enum):
    invoice_pkey= "invoice_pkey"

@dataclass(frozen=True)
class invoice_max_fields:
    created_at: datetime.datetime
    customer: UUID
    due_date: datetime.datetime
    invoice_number: str
    processing_status: str
    reference: UUID
    total_amount: int
    total_amount_currency: str
    updated_at: datetime.datetime    

    def to_json(self):
        res = {}
        if created_at is not None:
            res["created_at"] = self.created_at
        if customer is not None:
            res["customer"] = self.customer
        if due_date is not None:
            res["due_date"] = self.due_date
        if invoice_number is not None:
            res["invoice_number"] = self.invoice_number
        if processing_status is not None:
            res["processing_status"] = self.processing_status
        if reference is not None:
            res["reference"] = self.reference
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        if total_amount_currency is not None:
            res["total_amount_currency"] = self.total_amount_currency
        if updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = d["created_at"],
            customer = d["customer"],
            due_date = d["due_date"],
            invoice_number = d["invoice_number"],
            processing_status = d["processing_status"],
            reference = d["reference"],
            total_amount = d["total_amount"],
            total_amount_currency = d["total_amount_currency"],
            updated_at = d["updated_at"],
        )
        pass

@dataclass(frozen=True)
class invoice_min_fields:
    created_at: datetime.datetime
    customer: UUID
    due_date: datetime.datetime
    invoice_number: str
    processing_status: str
    reference: UUID
    total_amount: int
    total_amount_currency: str
    updated_at: datetime.datetime    

    def to_json(self):
        res = {}
        if created_at is not None:
            res["created_at"] = self.created_at
        if customer is not None:
            res["customer"] = self.customer
        if due_date is not None:
            res["due_date"] = self.due_date
        if invoice_number is not None:
            res["invoice_number"] = self.invoice_number
        if processing_status is not None:
            res["processing_status"] = self.processing_status
        if reference is not None:
            res["reference"] = self.reference
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        if total_amount_currency is not None:
            res["total_amount_currency"] = self.total_amount_currency
        if updated_at is not None:
            res["updated_at"] = self.updated_at
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            created_at = d["created_at"],
            customer = d["customer"],
            due_date = d["due_date"],
            invoice_number = d["invoice_number"],
            processing_status = d["processing_status"],
            reference = d["reference"],
            total_amount = d["total_amount"],
            total_amount_currency = d["total_amount_currency"],
            updated_at = d["updated_at"],
        )
        pass

@dataclass(frozen=True)
class invoice_mutation_response:
    affected_rows: Optional[int] = None
    returning: Optional[List[invoice]] = None    

    def to_json(self):
        res = {}
        if affected_rows is not None:
            res["affected_rows"] = self.affected_rows
        if returning is not None:
            res["returning"] = self.returning
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            affected_rows = d["affected_rows"],
            returning = d["returning"],
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
    total_amount: float    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
        )
        pass

@dataclass(frozen=True)
class invoice_stddev_pop_fields:
    total_amount: float    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
        )
        pass

@dataclass(frozen=True)
class invoice_stddev_samp_fields:
    total_amount: float    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
        )
        pass

@dataclass(frozen=True)
class invoice_sum_fields:
    total_amount: int    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
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
    total_amount: float    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
        )
        pass

@dataclass(frozen=True)
class invoice_var_samp_fields:
    total_amount: float    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
        )
        pass

@dataclass(frozen=True)
class invoice_variance_fields:
    total_amount: float    

    def to_json(self):
        res = {}
        if total_amount is not None:
            res["total_amount"] = self.total_amount
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            total_amount = d["total_amount"],
        )
        pass

@dataclass(frozen=True)
class mutation_root:
    delete_customer: customer_mutation_response
    delete_customer_by_pk: customer
    delete_invoice: invoice_mutation_response
    delete_invoice_by_pk: invoice
    insert_customer: customer_mutation_response
    insert_customer_one: customer
    insert_invoice: invoice_mutation_response
    insert_invoice_one: invoice
    update_customer: customer_mutation_response
    update_customer_by_pk: customer
    update_invoice: invoice_mutation_response
    update_invoice_by_pk: invoice    

    def to_json(self):
        res = {}
        if delete_customer is not None:
            res["delete_customer"] = self.delete_customer
        if delete_customer_by_pk is not None:
            res["delete_customer_by_pk"] = self.delete_customer_by_pk
        if delete_invoice is not None:
            res["delete_invoice"] = self.delete_invoice
        if delete_invoice_by_pk is not None:
            res["delete_invoice_by_pk"] = self.delete_invoice_by_pk
        if insert_customer is not None:
            res["insert_customer"] = self.insert_customer
        if insert_customer_one is not None:
            res["insert_customer_one"] = self.insert_customer_one
        if insert_invoice is not None:
            res["insert_invoice"] = self.insert_invoice
        if insert_invoice_one is not None:
            res["insert_invoice_one"] = self.insert_invoice_one
        if update_customer is not None:
            res["update_customer"] = self.update_customer
        if update_customer_by_pk is not None:
            res["update_customer_by_pk"] = self.update_customer_by_pk
        if update_invoice is not None:
            res["update_invoice"] = self.update_invoice
        if update_invoice_by_pk is not None:
            res["update_invoice_by_pk"] = self.update_invoice_by_pk
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            delete_customer = d["delete_customer"],
            delete_customer_by_pk = d["delete_customer_by_pk"],
            delete_invoice = d["delete_invoice"],
            delete_invoice_by_pk = d["delete_invoice_by_pk"],
            insert_customer = d["insert_customer"],
            insert_customer_one = d["insert_customer_one"],
            insert_invoice = d["insert_invoice"],
            insert_invoice_one = d["insert_invoice_one"],
            update_customer = d["update_customer"],
            update_customer_by_pk = d["update_customer_by_pk"],
            update_invoice = d["update_invoice"],
            update_invoice_by_pk = d["update_invoice_by_pk"],
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
    customer_by_pk: customer
    invoice_by_pk: invoice
    customer: Optional[List[customer]] = None
    customer_aggregate: Optional[customer_aggregate] = None
    invoice: Optional[List[invoice]] = None
    invoice_aggregate: Optional[invoice_aggregate] = None    

    def to_json(self):
        res = {}
        if customer is not None:
            res["customer"] = self.customer
        if customer_aggregate is not None:
            res["customer_aggregate"] = self.customer_aggregate
        if customer_by_pk is not None:
            res["customer_by_pk"] = self.customer_by_pk
        if invoice is not None:
            res["invoice"] = self.invoice
        if invoice_aggregate is not None:
            res["invoice_aggregate"] = self.invoice_aggregate
        if invoice_by_pk is not None:
            res["invoice_by_pk"] = self.invoice_by_pk
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            customer = d["customer"],
            customer_aggregate = d["customer_aggregate"],
            customer_by_pk = d["customer_by_pk"],
            invoice = d["invoice"],
            invoice_aggregate = d["invoice_aggregate"],
            invoice_by_pk = d["invoice_by_pk"],
        )
        pass

@dataclass(frozen=True)
class subscription_root:
    customer_by_pk: customer
    invoice_by_pk: invoice
    customer: Optional[List[customer]] = None
    customer_aggregate: Optional[customer_aggregate] = None
    invoice: Optional[List[invoice]] = None
    invoice_aggregate: Optional[invoice_aggregate] = None    

    def to_json(self):
        res = {}
        if customer is not None:
            res["customer"] = self.customer
        if customer_aggregate is not None:
            res["customer_aggregate"] = self.customer_aggregate
        if customer_by_pk is not None:
            res["customer_by_pk"] = self.customer_by_pk
        if invoice is not None:
            res["invoice"] = self.invoice
        if invoice_aggregate is not None:
            res["invoice_aggregate"] = self.invoice_aggregate
        if invoice_by_pk is not None:
            res["invoice_by_pk"] = self.invoice_by_pk
        return res
        pass

    @classmethod
    def from_json(cls, d):
        return cls(
            customer = d["customer"],
            customer_aggregate = d["customer_aggregate"],
            customer_by_pk = d["customer_by_pk"],
            invoice = d["invoice"],
            invoice_aggregate = d["invoice_aggregate"],
            invoice_by_pk = d["invoice_by_pk"],
        )
        pass


