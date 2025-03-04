package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseItemRepositoryCustom {
    Page<PurchaseItem> searchForPurchaseItem(
            UUID clientId,
            Long purchaseNumber,
            String warehouse,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
