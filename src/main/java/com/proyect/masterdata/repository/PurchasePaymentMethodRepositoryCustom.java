package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchasePaymentMethod;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface PurchasePaymentMethodRepositoryCustom {
    Page<PurchasePaymentMethod> searchForPurchasePaymentMethod(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
