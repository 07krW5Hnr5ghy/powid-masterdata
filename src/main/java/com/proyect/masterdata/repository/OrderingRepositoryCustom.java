package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderingRepositoryCustom {
    Page<Ordering> searchForOrdering(
            Long id,
            User user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
