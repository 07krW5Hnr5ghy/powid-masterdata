package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancellationReason;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;

@Repository
public interface CancellationReasonRepositoryCustom {
    Page<CancellationReason> searchForCancellationReason(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
