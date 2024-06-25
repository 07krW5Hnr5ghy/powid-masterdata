package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseItemRepositoryCustom {
    Page<PurchaseItem> searchForPurchaseItem(
            Long clientId,
            List<Long> purchaseIds,
            List<Long> supplierIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
