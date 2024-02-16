package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockReplenishment;
import com.proyect.masterdata.repository.StockReplenishmentRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class StockReplenishmentRepositoryCustomImpl implements StockReplenishmentRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<StockReplenishment> searchForStockReplenishment(Long clientId, Long orderId, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockReplenishment> criteriaQuery = criteriaBuilder.createQuery(StockReplenishment.class);
        Root<StockReplenishment> itemRoot = criteriaQuery.from(StockReplenishment.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(clientId,orderId,status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> stockReplenishmentList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                stockReplenishmentList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                stockReplenishmentList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(stockReplenishmentList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockReplenishment> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(clientId,orderId, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicate(
            Long clientId,
            Long orderId,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<StockReplenishment> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (orderId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderId"), orderId)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReplenishment> itemRoot) {

        List<Order> stockReplenishmentList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReplenishmentList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("orderId")) {
            stockReplenishmentList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }

        return stockReplenishmentList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReplenishment> itemRoot) {

        List<Order> stockReplenishmentList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReplenishmentList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("orderId")) {
            stockReplenishmentList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }

        return stockReplenishmentList;
    }

    private Long getOrderCount(Long clientId, Long orderId,Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockReplenishment> itemRoot = criteriaQuery.from(StockReplenishment.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(clientId, orderId, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
