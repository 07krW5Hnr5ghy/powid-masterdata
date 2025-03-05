package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.WarehouseOutput;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;

@Repository
public interface WarehouseOutputRepositoryCustom {
    Page<WarehouseOutput> searchForWarehouseOutput(
            String name,
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
