package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseDiscount;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface PurchaseDiscountRepositoryCustom {
    Page<PurchaseDiscount> searchForPurchaseDiscount(
            UUID clientId,
            String name,
            Double value,
            Boolean percentage,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
