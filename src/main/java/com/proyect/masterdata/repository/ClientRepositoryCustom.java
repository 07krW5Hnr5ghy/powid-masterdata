package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Client;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;

@Repository
public interface ClientRepositoryCustom {
    Page<Client> searchForClient(
            String ruc,
            String business,
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
