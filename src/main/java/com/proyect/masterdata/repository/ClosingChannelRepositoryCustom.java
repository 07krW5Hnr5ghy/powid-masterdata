package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ClosingChannel;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ClosingChannelRepositoryCustom {
    Page<ClosingChannel> searchForClosingChannel(
            String name,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
