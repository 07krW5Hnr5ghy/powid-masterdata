package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Shipment;
import com.proyect.masterdata.repository.ShipmentRepositoryCustom;
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
public class ShipmentRepositoryCustomImpl implements ShipmentRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Shipment> searchForShipment(Long clientId, String purchaseSerial, Long warehouseId,Long shipmentTypeId, String sort, String sortColumn, Integer pageNumber, Integer pageSize,Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Shipment> criteriaQuery = criteriaBuilder.createQuery(Shipment.class);

        Root<Shipment> itemRoot = criteriaQuery.from(Shipment.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(clientId, purchaseSerial, warehouseId,shipmentTypeId,status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> shipmentList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                shipmentList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                shipmentList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(shipmentList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Shipment> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(clientId, purchaseSerial, warehouseId,shipmentTypeId,status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(Long clientId, String purchaseSerial, Long warehouseId,Long shipmentTypeId,Boolean status, CriteriaBuilder criteriaBuilder,
                                      Root<Shipment> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (purchaseSerial != null) {
            conditions.add(criteriaBuilder
                    .and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("purchaseSerial")), purchaseSerial.toUpperCase())));
        }

        if (warehouseId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("warehouseId"), warehouseId)));
        }

        if (shipmentTypeId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("shipmentTypeId"), shipmentTypeId)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Shipment> itemRoot) {

        List<Order> shipmentList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            shipmentList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            shipmentList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("shipmentTypeId")) {
            shipmentList.add(criteriaBuilder.asc(itemRoot.get("shipmentTypeId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseSerial")) {
            shipmentList.add(criteriaBuilder.asc(itemRoot.get("purchaseSerial")));
        }

        return shipmentList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Shipment> itemRoot) {

        List<Order> shipmentList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            shipmentList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            shipmentList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }

        if (sortColumn.equalsIgnoreCase("shipmentTypeId")) {
            shipmentList.add(criteriaBuilder.desc(itemRoot.get("shipmentTypeId")));
        }

        if (sortColumn.equalsIgnoreCase("purchaseSerial")) {
            shipmentList.add(criteriaBuilder.desc(itemRoot.get("purchaseSerial")));
        }

        return shipmentList;
    }

    private Long getOrderCount(Long clientId, String purchaseSerial, Long warehouseId,Long shipmentTypeId,Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Shipment> itemRoot = criteriaQuery.from(Shipment.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(clientId, purchaseSerial, warehouseId,shipmentTypeId,status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
