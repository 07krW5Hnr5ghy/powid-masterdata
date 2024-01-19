package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.WarehouseStock;
import com.proyect.masterdata.repository.WarehouseStockRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Repository
public class WarehouseStockRepositoryCustomImpl implements WarehouseStockRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<WarehouseStock> searchForWarehouseStock(Long clientId, Long warehouseId, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<WarehouseStock> criteriaQuery = criteriaBuilder.createQuery(WarehouseStock.class);
        Root<WarehouseStock> itemRoot = criteriaQuery.from(WarehouseStock.class);
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(warehouseId, clientId, criteriaBuilder, itemRoot);

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

        TypedQuery<WarehouseStock> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(warehouseId, clientId);

        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);

    }

    private List<Predicate> predicate(Long warehouseId, Long clientId, CriteriaBuilder criteriaBuilder,
            Root<WarehouseStock> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (warehouseId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("warehouseId"), warehouseId)));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseStock> itemRoot) {

        List<Order> warehouseStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseStockList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            warehouseStockList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        return warehouseStockList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseStock> itemRoot) {

        List<Order> warehouseStockList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseStockList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            warehouseStockList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        return warehouseStockList;

    }

    private Long getOrderCount(Long warehouseId, Long clientId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<WarehouseStock> itemRoot = criteriaQuery.from(WarehouseStock.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(warehouseId, clientId, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
