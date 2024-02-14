package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import com.proyect.masterdata.domain.ShipmentItem;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.repository.ShipmentItemRepositoryCustom;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Repository
public class ShipmentItemRepositoryCustomImpl implements ShipmentItemRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<ShipmentItem> searchForShipmentItem(Long clientId, Long shipmentId, Long supplierProductId, String sort,
                                                String sortColumn, Integer pageNumber, Integer pageSize) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<ShipmentItem> criteriaQuery = criteriaBuilder.createQuery(ShipmentItem.class);

        Root<ShipmentItem> itemRoot = criteriaQuery.from(ShipmentItem.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(clientId, shipmentId, supplierProductId, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> shipmentItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                shipmentItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                shipmentItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(shipmentItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<ShipmentItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(clientId, shipmentId, supplierProductId);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(Long clientId, Long shipmentId, Long supplierProductId, CriteriaBuilder criteriaBuilder,
            Root<ShipmentItem> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (shipmentId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("shipmentId"), shipmentId)));
        }

        if (supplierProductId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("supplierProductId"), shipmentId)));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<ShipmentItem> itemRoot) {

        List<Order> shipmentItemList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            shipmentItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("shipmentId")) {
            shipmentItemList.add(criteriaBuilder.asc(itemRoot.get("shipmentId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            shipmentItemList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        return shipmentItemList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<ShipmentItem> itemRoot) {

        List<Order> shipmentList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            shipmentList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("shipmentId")) {
            shipmentList.add(criteriaBuilder.desc(itemRoot.get("shipmentId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            shipmentList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        return shipmentList;
    }

    private Long getOrderCount(Long clientId, Long shipmentId, Long supplierProductId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<ShipmentItem> itemRoot = criteriaQuery.from(ShipmentItem.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(clientId, shipmentId, supplierProductId, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

}
