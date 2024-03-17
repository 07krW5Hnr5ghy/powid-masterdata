package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface PurchaseRepositoryCustom {
    Page<Purchase> searchForPurchase(
            Long clientId,
            String serial,
            Long purchaseDocumentId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
