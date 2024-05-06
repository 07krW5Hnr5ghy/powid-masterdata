package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.EntryChannel;
import com.proyect.masterdata.repository.EntryChannelRepositoryCustom;
import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class EntryChannelRepositoryCustomImpl implements EntryChannelRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<EntryChannel> searchEntryChannel(String name, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<EntryChannel> criteriaQuery = criteriaBuilder.createQuery(EntryChannel.class);
        Root<EntryChannel> itemRoot = criteriaQuery.from(EntryChannel.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(name, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> entryChannelList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                entryChannelList = listAsc(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                entryChannelList = listDesc(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(entryChannelList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<EntryChannel> orderTypeQuery = entityManager.createQuery(criteriaQuery);
        orderTypeQuery.setFirstResult(pageNumber * pageSize);
        orderTypeQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(name, status);
        return new PageImpl<>(orderTypeQuery.getResultList(), pageable, count);
    }
    public List<Predicate> predicateConditions(
            String name,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<EntryChannel> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listAsc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<EntryChannel> itemRoot) {

        List<Order> entryChannelList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            entryChannelList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        return entryChannelList;
    }

    List<Order> listDesc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<EntryChannel> itemRoot) {
        List<Order> entryChannelList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            entryChannelList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        return entryChannelList;
    }

    private long getOrderCount(String name, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<EntryChannel> itemRoot = criteriaQuery.from(EntryChannel.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
