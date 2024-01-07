package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Purchase;

@Repository
public interface PurchaseRepositoryCustom {
    Page<Purchase> searchForPurchase(
            Long clientId,
            String serial,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
