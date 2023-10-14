package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Channel;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ChannelRepositoryCustom {
    Page<Channel> searchForChannel(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
