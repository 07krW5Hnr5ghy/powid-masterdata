package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.State;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StateRepositoryCustom {
    Page<State> searchForState(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
