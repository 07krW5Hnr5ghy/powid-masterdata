package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.GeneralStock;
import com.proyect.masterdata.repository.GeneralStockRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Repository
public class GeneralStockRepositoryCustomImpl implements GeneralStockRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<GeneralStock> searchForGeneralStock(Long clientId, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<GeneralStock> criteriaQuery = criteriaBuilder.createQuery(GeneralStock.class);
        Root<GeneralStock> itemRoot = criteriaQuery.from(GeneralStock.class);
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(clientId, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> warehouseStockList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                warehouseStockList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                warehouseStockList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(warehouseStockList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<GeneralStock> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(clientId);

        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);

    }

    private List<Predicate> predicate(Long clientId, CriteriaBuilder criteriaBuilder,
            Root<GeneralStock> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<GeneralStock> itemRoot) {

        List<Order> generalStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            generalStockList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return generalStockList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<GeneralStock> itemRoot) {

        List<Order> generalStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            generalStockList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return generalStockList;

    }

    private Long getOrderCount(Long clientId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<GeneralStock> itemRoot = criteriaQuery.from(GeneralStock.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(clientId, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
