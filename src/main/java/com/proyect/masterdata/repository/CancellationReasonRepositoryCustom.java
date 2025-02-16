package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancellationReason;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;

@Repository
public interface CancellationReasonRepositoryCustom {
    Page<CancellationReason> searchForCancellationReason(
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
