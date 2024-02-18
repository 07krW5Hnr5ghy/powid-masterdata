package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderingRepositoryCustom {
    Page<Ordering> searchForOrdering(
            Long orderId,
            Long clientId,
            Long orderStateId,
            Long courierId,
            Long paymentStateId,
            Long paymentMethodId,
            Long saleChannelId,
            Long managementTypeId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
