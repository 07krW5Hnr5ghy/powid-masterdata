package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturn;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public interface OrderReturnRepositoryCustom {
    Page<OrderReturn> searchForOrderReturn(
            UUID clientId,
            List<UUID> orderIds,
            List<UUID> warehouseIds,
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
