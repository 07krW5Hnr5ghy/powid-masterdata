package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Model;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface ModelRepositoryCustom {
    Page<Model> searchForModel(
            UUID clientId,
            String name,
            List<UUID> brandIds,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
