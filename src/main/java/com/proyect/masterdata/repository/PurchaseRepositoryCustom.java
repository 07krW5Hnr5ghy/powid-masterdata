package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseRepositoryCustom {
    Page<Purchase> searchForPurchase(
            Long clientId,
            List<String> serial,
            List<Long> purchaseDocumentIds,
            List<Long> supplierProductIds,
            List<Long> supplierIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
