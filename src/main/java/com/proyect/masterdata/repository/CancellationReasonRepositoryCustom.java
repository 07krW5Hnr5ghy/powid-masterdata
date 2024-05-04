package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancellationReason;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface CancellationReasonRepositoryCustom {
    Page<CancellationReason> searchForCancellationReason(
            String name,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
