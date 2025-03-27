package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderContacted;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface OrderContactedRepositoryCustom {
    Page<OrderContacted> searchForContactedOrder(
            UUID clientId,
            Long orderNumber,
            String deliveryZone,
            Boolean contacted,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
